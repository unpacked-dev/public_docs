# The Signalservice 
is a service for sending and recieving updates in Angular between components.

## Implementing the signalservice
1. Create a new service in your **services** folder.

`signal.service.ts`
```ts
import { Injectable } from '@angular/core';
import { BehaviorSubject, Observable } from 'rxjs';
import { filter } from 'rxjs/operators';

export enum Signal{
  None,
  UpdateCustomerList,
  UpdateBadgeList,
  //...Define more services
}

@Injectable()
export class SignalService {

  private sender: any = new BehaviorSubject(Signal.None);
  private signal$: Observable<Signal> = this.sender.asObservable();

  constructor() { }

  public sendSignal(signal: Signal) {
    this.sender.next(signal);
  }

  public onSignal(filterSignal?: Signal): Observable<Signal> {
    return this.signal$.pipe(
      filter((signal) => filterSignal ? filterSignal === signal : true),
    );
  }

}
```

2. Add the Signalservice to your component

`a.component.ts`
```ts
constructor(
    private signalService: SignalService,
){ }

//...

ngOnInit() {
    //Subsribe to the Signal Event
    this.signalService
        .onSignal(Signal.UpdateCustomerList)
        .subscribe((signal) => {
            this.loadCustomerList(true);
    });
}
```

3. Send a message

`b.component.ts`
```ts
//...

this.signalService.sendSignal(Signal.UpdateCustomerList);
```

© unpacked - [licence](../../LICENSE) <br>
Thanks to [Sophie](https://github.com/sooophies)