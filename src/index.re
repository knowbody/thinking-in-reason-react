[%bs.raw {|require('./index.css')|}];

type product = {
  id: int,
  category: string,
  name: string,
  price: float,
  stocked: bool
};

module ProductCategoryRow = {
  let component = ReasonReact.statelessComponent("ProductCategoryRow");
  let make = (~category, _children) => {
    ...component,
    render: _self =>
      <tr> <th colSpan=2> (ReasonReact.stringToElement(category)) </th> </tr>
  };
};

module ProductRow = {
  let component = ReasonReact.statelessComponent("Product");
  let make = (~name, ~price, ~stocked, _children) => {
    ...component,
    render: _self => {
      let productName =
        stocked ?
          ReasonReact.stringToElement(name) :
          <span style=(ReactDOMRe.Style.make(~color="red", ()))>
            (ReasonReact.stringToElement(name))
          </span>;
      <tr>
        <td> productName </td>
        <td> (ReasonReact.stringToElement("$" ++ string_of_float(price))) </td>
      </tr>;
    }
  };
};

module ProductTable = {
  let component = ReasonReact.statelessComponent("ProductTable");
  let make = (~products, ~inStockOnly, ~filterText, _children) => {
    ...component,
    render: _self => {
      let currentCategory = ref("");
      let productRows =
        products
        |> Array.map(({id, name, price, stocked, category}) =>
             if (inStockOnly && ! stocked) {
               ReasonReact.nullElement;
             } else if (Js.String.indexOf(filterText, String.lowercase(name))
                        === (-1)) {
               ReasonReact.nullElement;
             } else if (category !== currentCategory^) {
               currentCategory := category;
               [|
                 <ProductCategoryRow category key=category />,
                 <ProductRow key=(string_of_int(id)) name price stocked />
               |]
               |> ReasonReact.arrayToElement;
             } else {
               <ProductRow key=(string_of_int(id)) name price stocked />;
             }
           )
        |> ReasonReact.arrayToElement;
      <table>
        <thead>
          <tr>
            <th> (ReasonReact.stringToElement("Name")) </th>
            <th> (ReasonReact.stringToElement("Price")) </th>
          </tr>
        </thead>
        <tbody> productRows </tbody>
      </table>;
    }
  };
};

module SearchBar = {
  let component = ReasonReact.statelessComponent("SearchBar");
  let make =
      (
        ~inStockOnly,
        ~onInStockChange,
        ~filterText,
        ~handleFilterTextChange,
        _children
      ) => {
    ...component,
    render: _self =>
      <form>
        <input
          _type="text"
          placeholder="Search..."
          value=filterText
          onChange=(
            e =>
              handleFilterTextChange(
                ReactDOMRe.domElementToObj(ReactEventRe.Form.target(e))##value
              )
          )
        />
        <p>
          <input
            _type="checkbox"
            checked=inStockOnly
            onChange=(_e => onInStockChange())
          />
          (ReasonReact.stringToElement("Only show products in stock"))
        </p>
      </form>
  };
};

module FilterableProductTable = {
  type action =
    | HandleInStockChange
    | HandleFilterTextChange(string);
  type state = {
    inStockOnly: bool,
    filterText: string
  };
  let component = ReasonReact.reducerComponent("FilterableProductTable");
  let make = (~products, _children) => {
    ...component,
    initialState: () => {inStockOnly: false, filterText: ""},
    reducer: (action, state) =>
      switch action {
      | HandleInStockChange =>
        ReasonReact.Update({...state, inStockOnly: ! state.inStockOnly})
      | HandleFilterTextChange(filterText) =>
        ReasonReact.Update({...state, filterText})
      },
    render: self =>
      <div>
        <SearchBar
          inStockOnly=(Js.Boolean.to_js_boolean(self.state.inStockOnly))
          onInStockChange=(() => self.send(HandleInStockChange))
          filterText=self.state.filterText
          handleFilterTextChange=(
            filterText => self.send(HandleFilterTextChange(filterText))
          )
        />
        <ProductTable
          products
          inStockOnly=self.state.inStockOnly
          filterText=self.state.filterText
        />
      </div>
  };
};

let products = [|
  {
    id: 1,
    category: "Sporting Goods",
    price: 49.99,
    stocked: true,
    name: "Football"
  },
  {
    id: 2,
    category: "Sporting Goods",
    price: 9.99,
    stocked: true,
    name: "Baseball"
  },
  {
    id: 3,
    category: "Sporting Goods",
    price: 29.99,
    stocked: false,
    name: "Basketball"
  },
  {
    id: 6,
    category: "Sporting Goods",
    price: 199.99,
    stocked: true,
    name: "Nexus 7"
  },
  {
    id: 4,
    category: "Electronics",
    price: 99.99,
    stocked: true,
    name: "iPod Touch"
  },
  {
    id: 5,
    category: "Electronics",
    price: 399.99,
    stocked: false,
    name: "iPhone 5"
  }
|];

ReactDOMRe.renderToElementWithId(<FilterableProductTable products />, "root");